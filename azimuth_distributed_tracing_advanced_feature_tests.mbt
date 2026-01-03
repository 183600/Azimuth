// Azimuth 分布式链路追踪高级功能测试
// 专注于测试分布式系统中的复杂链路追踪场景

// 测试1: 跨服务链路追踪
test "跨服务链路追踪测试" {
  // 1. 创建根追踪上下文
  let root_trace_context = TraceContext({
    trace_id: "trace-abc123def456",
    span_id: "span-111111111111",
    parent_span_id: None,
    sampled: true,
    baggage: [
      ("user.id", "user-12345"),
      ("request.id", "req-67890"),
      ("correlation.id", "corr-abcde")
    ],
    trace_flags: 0x01
  })
  
  // 2. 验证根追踪上下文
  assert_eq(root_trace_context.trace_id, "trace-abc123def456")
  assert_eq(root_trace_context.span_id, "span-111111111111")
  assert_eq(root_trace_context.parent_span_id, None)
  assert_eq(root_trace_context.sampled, true)
  assert_eq(root_trace_context.baggage.length(), 3)
  assert_eq(root_trace_context.trace_flags, 0x01)
  
  // 3. 创建服务A的span
  let service_a_span = create_child_span(root_trace_context, "service-a.process", [
    ("service.name", "service-a"),
    ("service.version", "1.2.3"),
    ("operation.type", "http.request")
  ])
  
  // 4. 验证服务A的span
  assert_eq(service_a_span.trace_id, root_trace_context.trace_id)
  assert_eq(service_a_span.parent_span_id, Some(root_trace_context.span_id))
  assert_eq(service_a_span.operation_name, "service-a.process")
  assert_eq(service_a_span.attributes.length(), 3)
  
  // 5. 创建服务B的span（从服务A调用）
  let service_b_span = create_child_span(service_a_span, "service-b.validate", [
    ("service.name", "service-b"),
    ("service.version", "2.1.0"),
    ("operation.type", "rpc.call"),
    ("rpc.service", "validation.service")
  ])
  
  // 6. 验证服务B的span
  assert_eq(service_b_span.trace_id, root_trace_context.trace_id)
  assert_eq(service_b_span.parent_span_id, Some(service_a_span.span_id))
  assert_eq(service_b_span.operation_name, "service-b.validate")
  assert_eq(service_b_span.attributes.length(), 4)
  
  // 7. 创建服务C的span（从服务B调用）
  let service_c_span = create_child_span(service_b_span, "service-c.transform", [
    ("service.name", "service-c"),
    ("service.version", "3.0.1"),
    ("operation.type", "message.process"),
    ("message.queue", "transform.queue")
  ])
  
  // 8. 验证服务C的span
  assert_eq(service_c_span.trace_id, root_trace_context.trace_id)
  assert_eq(service_c_span.parent_span_id, Some(service_b_span.span_id))
  assert_eq(service_c_span.operation_name, "service-c.transform")
  assert_eq(service_c_span.attributes.length(), 4)
  
  // 9. 创建span链
  let trace_chain = [root_trace_context, service_a_span, service_b_span, service_c_span]
  
  // 10. 验证span链的连续性
  for i in 1..trace_chain.length() {
    let parent = trace_chain[i-1]
    let child = trace_chain[i]
    
    assert_eq(parent.trace_id, child.trace_id)
    assert_eq(child.parent_span_id, Some(parent.span_id))
    assert_true(child.timestamp >= parent.timestamp)
  }
  
  // 11. 验证baggage传播
  for span in trace_chain {
    assert_eq(span.baggage.length(), 3)
    assert_true(span.baggage.contains fn(item) { item.0 == "user.id" && item.1 == "user-12345" })
    assert_true(span.baggage.contains fn(item) { item.0 == "request.id" && item.1 == "req-67890" })
    assert_true(span.baggage.contains fn(item) { item.0 == "correlation.id" && item.1 == "corr-abcde" })
  }
}

// 测试2: 异步操作追踪
test "异步操作追踪测试" {
  // 1. 创建异步操作的根span
  let async_root_span = Span({
    trace_id: "trace-async123456",
    span_id: "span-async-root",
    parent_span_id: None,
    operation_name: "async.operation.root",
    span_kind: Server,
    timestamp: 1640995200000,
    duration: None,  // 运行中
    status: Ok,
    attributes: [
      ("operation.type", "async.process"),
      ("async.operation.id", "async-op-123")
    ],
    events: [],
    links: []
  })
  
  // 2. 验证异步根span
  assert_eq(async_root_span.operation_name, "async.operation.root")
  assert_eq(async_root_span.span_kind, Server)
  assert_eq(async_root_span.duration, None)
  assert_eq(async_root_span.status, Ok)
  
  // 3. 创建异步子操作span
  let async_child_spans = [
    Span({
      trace_id: async_root_span.trace_id,
      span_id: "span-async-child-1",
      parent_span_id: Some(async_root_span.span_id),
      operation_name: "async.database.query",
      span_kind: Client,
      timestamp: async_root_span.timestamp + 100,
      duration: Some(500),
      status: Ok,
      attributes: [
        ("operation.type", "async.database"),
        ("db.statement", "SELECT * FROM users WHERE id = ?"),
        ("db.type", "postgresql")
      ],
      events: [
        SpanEvent({
          timestamp: async_root_span.timestamp + 200,
          name: "db.query.start",
          attributes: []
        }),
        SpanEvent({
          timestamp: async_root_span.timestamp + 600,
          name: "db.query.complete",
          attributes: [("rows.affected", "1")]
        })
      ],
      links: []
    }),
    Span({
      trace_id: async_root_span.trace_id,
      span_id: "span-async-child-2",
      parent_span_id: Some(async_root_span.span_id),
      operation_name: "async.cache.lookup",
      span_kind: Internal,
      timestamp: async_root_span.timestamp + 150,
      duration: Some(50),
      status: Ok,
      attributes: [
        ("operation.type", "async.cache"),
        ("cache.key", "user:123"),
        ("cache.hit", "true")
      ],
      events: [],
      links: []
    }),
    Span({
      trace_id: async_root_span.trace_id,
      span_id: "span-async-child-3",
      parent_span_id: Some(async_root_span.span_id),
      operation_name: "async.external.api",
      span_kind: Client,
      timestamp: async_root_span.timestamp + 200,
      duration: Some(800),
      status: Error,
      attributes: [
        ("operation.type", "async.http"),
        ("http.method", "GET"),
        ("http.url", "https://api.external.com/users/123"),
        ("http.status_code", "500")
      ],
      events: [
        SpanEvent({
          timestamp: async_root_span.timestamp + 1000,
          name: "error",
          attributes: [
            ("error.type", "http.error"),
            ("error.message", "Internal Server Error")
          ]
        })
      ],
      links: []
    })
  ]
  
  // 4. 验证异步子操作span
  assert_eq(async_child_spans.length(), 3)
  
  for child_span in async_child_spans {
    assert_eq(child_span.trace_id, async_root_span.trace_id)
    assert_eq(child_span.parent_span_id, Some(async_root_span.span_id))
    assert_true(child_span.timestamp >= async_root_span.timestamp)
  }
  
  // 5. 验证特定子span的属性
  let db_span = async_child_spans[0]
  assert_eq(db_span.operation_name, "async.database.query")
  assert_eq(db_span.span_kind, Client)
  assert_eq(db_span.duration, Some(500))
  assert_eq(db_span.events.length(), 2)
  
  let cache_span = async_child_spans[1]
  assert_eq(cache_span.operation_name, "async.cache.lookup")
  assert_eq(cache_span.span_kind, Internal)
  assert_eq(cache_span.duration, Some(50))
  assert_true(cache_span.attributes.contains fn(attr) { 
    attr.0 == "cache.hit" && attr.1 == "true" 
  })
  
  let api_span = async_child_spans[2]
  assert_eq(api_span.operation_name, "async.external.api")
  assert_eq(api_span.span_kind, Client)
  assert_eq(api_span.duration, Some(800))
  assert_eq(api_span.status, Error)
  assert_eq(api_span.events.length(), 1)
  
  // 6. 完成根span
  let completed_root_span = { async_root_span | 
    duration = Some(1000),
    events = [
      SpanEvent({
        timestamp: async_root_span.timestamp + 100,
        name: "async.operation.started",
        attributes: []
      }),
      SpanEvent({
        timestamp: async_root_span.timestamp + 1100,
        name: "async.operation.completed",
        attributes: [
          ("async.children.count", "3"),
          ("async.success.count", "2"),
          ("async.error.count", "1")
        ]
      })
    ]
  }
  
  // 7. 验证完成的根span
  assert_eq(completed_root_span.duration, Some(1000))
  assert_eq(completed_root_span.events.length(), 2)
  
  // 8. 构建完整的异步追踪树
  let async_trace_tree = TraceTree({
    root_span: completed_root_span,
    child_spans: async_child_spans
  })
  
  // 9. 验证异步追踪树
  assert_eq(async_trace_tree.root_span.span_id, "span-async-root")
  assert_eq(async_trace_tree.child_spans.length(), 3)
  
  // 10. 计算异步操作统计
  let async_stats = calculate_async_statistics(async_trace_tree)
  
  // 11. 验证异步操作统计
  assert_eq(async_stats.total_spans, 4)
  assert_eq(async_stats.root_span_duration, 1000)
  assert_eq(async_stats.successful_operations, 3)
  assert_eq(async_stats.failed_operations, 1)
  assert_eq(async_stats.average_child_duration, 450)  # (500 + 50 + 800) / 3
}

// 测试3: 批处理操作追踪
test "批处理操作追踪测试" {
  // 1. 创建批处理操作的根span
  let batch_root_span = Span({
    trace_id: "trace-batch789012",
    span_id: "span-batch-root",
    parent_span_id: None,
    operation_name: "batch.process.root",
    span_kind: Server,
    timestamp: 1640995200000,
    duration: None,
    status: Ok,
    attributes: [
      ("operation.type", "batch.process"),
      ("batch.id", "batch-12345"),
      ("batch.size", "1000"),
      ("batch.type", "user.data.import")
    ],
    events: [],
    links: []
  })
  
  // 2. 创建批处理子操作span
  let batch_item_spans = Array.range(0, 100).map(fn(i) {
    Span({
      trace_id: batch_root_span.trace_id,
      span_id: "span-batch-item-" + i.to_string(),
      parent_span_id: Some(batch_root_span.span_id),
      operation_name: "batch.item.process",
      span_kind: Internal,
      timestamp: batch_root_span.timestamp + i * 10,
      duration: Some(50 + (i % 100)),
      status: if i % 20 == 0 { Error } else { Ok },
      attributes: [
        ("operation.type", "batch.item"),
        ("batch.item.id", "item-" + i.to_string()),
        ("batch.item.index", i.to_string())
      ],
      events: if i % 20 == 0 {
        [SpanEvent({
          timestamp: batch_root_span.timestamp + i * 10 + 25,
          name: "error",
          attributes: [
            ("error.type", "processing.error"),
            ("error.message", "Invalid item data")
          ]
        })]
      } else { [] },
      links: []
    })
  })
  
  // 3. 验证批处理子操作span
  assert_eq(batch_item_spans.length(), 100)
  
  for i in 0..100 {
    let span = batch_item_spans[i]
    assert_eq(span.trace_id, batch_root_span.trace_id)
    assert_eq(span.parent_span_id, Some(batch_root_span.span_id))
    assert_eq(span.timestamp, batch_root_span.timestamp + i * 10)
    
    let expected_status = if i % 20 == 0 { Error } else { Ok }
    assert_eq(span.status, expected_status)
    
    let expected_events = if i % 20 == 0 { 1 } else { 0 }
    assert_eq(span.events.length(), expected_events)
  }
  
  // 4. 创建批处理阶段span
  let batch_phase_spans = [
    Span({
      trace_id: batch_root_span.trace_id,
      span_id: "span-batch-phase-validation",
      parent_span_id: Some(batch_root_span.span_id),
      operation_name: "batch.phase.validation",
      span_kind: Internal,
      timestamp: batch_root_span.timestamp + 50,
      duration: Some(500),
      status: Ok,
      attributes: [
        ("operation.type", "batch.phase"),
        ("batch.phase.name", "validation"),
        ("batch.phase.items.processed", "100")
      ],
      events: [],
      links: []
    }),
    Span({
      trace_id: batch_root_span.trace_id,
      span_id: "span-batch-phase-transformation",
      parent_span_id: Some(batch_root_span.span_id),
      operation_name: "batch.phase.transformation",
      span_kind: Internal,
      timestamp: batch_root_span.timestamp + 600,
      duration: Some(800),
      status: Ok,
      attributes: [
        ("operation.type", "batch.phase"),
        ("batch.phase.name", "transformation"),
        ("batch.phase.items.processed", "95")
      ],
      events: [],
      links: []
    }),
    Span({
      trace_id: batch_root_span.trace_id,
      span_id: "span-batch-phase-persistence",
      parent_span_id: Some(batch_root_span.span_id),
      operation_name: "batch.phase.persistence",
      span_kind: Client,
      timestamp: batch_root_span.timestamp + 1500,
      duration: Some(1200),
      status: Ok,
      attributes: [
        ("operation.type", "batch.phase"),
        ("batch.phase.name", "persistence"),
        ("batch.phase.items.processed", "95"),
        ("database.type", "postgresql")
      ],
      events: [],
      links: []
    })
  ]
  
  // 5. 验证批处理阶段span
  assert_eq(batch_phase_spans.length(), 3)
  
  for phase_span in batch_phase_spans {
    assert_eq(phase_span.trace_id, batch_root_span.trace_id)
    assert_eq(phase_span.parent_span_id, Some(batch_root_span.span_id))
    assert_true(phase_span.operation_name.starts_with("batch.phase."))
  }
  
  // 6. 完成批处理根span
  let completed_batch_root = { batch_root_span |
    duration = Some(3000),
    events = [
      SpanEvent({
        timestamp: batch_root_span.timestamp + 50,
        name: "batch.process.started",
        attributes: []
      }),
      SpanEvent({
        timestamp: batch_root_span.timestamp + 600,
        name: "batch.validation.completed",
        attributes: [("items.validated", "100")]
      }),
      SpanEvent({
        timestamp: batch_root_span.timestamp + 1500,
        name: "batch.transformation.completed",
        attributes: [("items.transformed", "95")]
      }),
      SpanEvent({
        timestamp: batch_root_span.timestamp + 2800,
        name: "batch.persistence.completed",
        attributes: [("items.persisted", "95")]
      }),
      SpanEvent({
        timestamp: batch_root_span.timestamp + 3000,
        name: "batch.process.completed",
        attributes: [
          ("total.items", "100"),
          ("successful.items", "95"),
          ("failed.items", "5")
        ]
      })
    ]
  }
  
  // 7. 构建批处理追踪树
  let batch_trace_tree = TraceTree({
    root_span: completed_batch_root,
    child_spans: batch_item_spans.concat(batch_phase_spans)
  })
  
  // 8. 验证批处理追踪树
  assert_eq(batch_trace_tree.root_span.span_id, "span-batch-root")
  assert_eq(batch_trace_tree.child_spans.length(), 103)  # 100 items + 3 phases
  
  // 9. 计算批处理统计
  let batch_stats = calculate_batch_statistics(batch_trace_tree)
  
  // 10. 验证批处理统计
  assert_eq(batch_stats.total_spans, 104)  # 1 root + 100 items + 3 phases
  assert_eq(batch_stats.total_items, 100)
  assert_eq(batch_stats.successful_items, 95)
  assert_eq(batch_stats.failed_items, 5)
  assert_eq(batch_stats.total_duration, 3000)
  assert_eq(batch_stats.phases_completed, 3)
  
  // 11. 验证批处理性能指标
  let throughput = batch_stats.total_items.to_float() / (batch_stats.total_duration.to_float() / 1000.0)
  assert_true(throughput >= 30.0 && throughput <= 35.0)  # 约33.33 items/sec
  
  let success_rate = batch_stats.successful_items.to_float() / batch_stats.total_items.to_float()
  assert_eq(success_rate, 0.95)
}

// 测试4: 微服务架构追踪
test "微服务架构追踪测试" {
  // 1. 创建API网关入口span
  let gateway_span = Span({
    trace_id: "trace-micro123456",
    span_id: "span-gateway-entry",
    parent_span_id: None,
    operation_name: "gateway.api.request",
    span_kind: Server,
    timestamp: 1640995200000,
    duration: Some(2500),
    status: Ok,
    attributes: [
      ("service.name", "api-gateway"),
      ("service.version", "2.1.0"),
      ("http.method", "POST"),
      ("http.route", "/api/v1/orders"),
      ("http.status_code", "200"),
      ("user.id", "user-12345"),
      ("client.ip", "192.168.1.100")
    ],
    events: [
      SpanEvent({
        timestamp: 1640995200000 + 100,
        name: "authentication.started",
        attributes: []
      }),
      SpanEvent({
        timestamp: 1640995200000 + 300,
        name: "authentication.completed",
        attributes: [("auth.result", "success")]
      }),
      SpanEvent({
        timestamp: 1640995200000 + 2400,
        name: "response.sent",
        attributes: [("response.size", "1024")]
      })
    ],
    links: []
  })
  
  // 2. 创建订单服务span
  let order_service_span = Span({
    trace_id: gateway_span.trace_id,
    span_id: "span-order-service",
    parent_span_id: Some(gateway_span.span_id),
    operation_name: "order.service.process",
    span_kind: Server,
    timestamp: gateway_span.timestamp + 400,
    duration: Some(1800),
    status: Ok,
    attributes: [
      ("service.name", "order-service"),
      ("service.version", "1.5.2"),
      ("operation.type", "order.creation"),
      ("order.id", "order-67890"),
      ("order.amount", "99.99"),
      ("order.currency", "USD")
    ],
    events: [
      SpanEvent({
        timestamp: gateway_span.timestamp + 500,
        name: "order.validation.started",
        attributes: []
      }),
      SpanEvent({
        timestamp: gateway_span.timestamp + 700,
        name: "order.validation.completed",
        attributes: [("validation.result", "success")]
      })
    ],
    links: []
  })
  
  // 3. 创建库存服务span
  let inventory_service_span = Span({
    trace_id: gateway_span.trace_id,
    span_id: "span-inventory-service",
    parent_span_id: Some(order_service_span.span_id),
    operation_name: "inventory.service.check",
    span_kind: Client,
    timestamp: order_service_span.timestamp + 800,
    duration: Some(300),
    status: Ok,
    attributes: [
      ("service.name", "inventory-service"),
      ("service.version", "1.2.1"),
      ("operation.type", "inventory.check"),
      ("product.id", "prod-12345"),
      ("quantity.requested", "1"),
      ("quantity.available", "10")
    ],
    events: [],
    links: []
  })
  
  // 4. 创建支付服务span
  let payment_service_span = Span({
    trace_id: gateway_span.trace_id,
    span_id: "span-payment-service",
    parent_span_id: Some(order_service_span.span_id),
    operation_name: "payment.service.process",
    span_kind: Client,
    timestamp: order_service_span.timestamp + 1200,
    duration: Some(800),
    status: Ok,
    attributes: [
      ("service.name", "payment-service"),
      ("service.version", "2.0.3"),
      ("operation.type", "payment.processing"),
      ("payment.id", "pay-11111"),
      ("payment.amount", "99.99"),
      ("payment.currency", "USD"),
      ("payment.method", "credit_card"),
      ("payment.gateway", "stripe")
    ],
    events: [
      SpanEvent({
        timestamp: order_service_span.timestamp + 1300,
        name: "payment.authorization.started",
        attributes: []
      }),
      SpanEvent({
        timestamp: order_service_span.timestamp + 1900,
        name: "payment.authorization.completed",
        attributes: [("auth.result", "approved")]
      })
    ],
    links: []
  })
  
  // 5. 创建通知服务span
  let notification_service_span = Span({
    trace_id: gateway_span.trace_id,
    span_id: "span-notification-service",
    parent_span_id: Some(order_service_span.span_id),
    operation_name: "notification.service.send",
    span_kind: Client,
    timestamp: order_service_span.timestamp + 2100,
    duration: Some(400),
    status: Ok,
    attributes: [
      ("service.name", "notification-service"),
      ("service.version", "1.0.5"),
      ("operation.type", "notification.email"),
      ("notification.id", "notif-22222"),
      ("notification.type", "email"),
      ("notification.recipient", "user@example.com"),
      ("notification.template", "order.confirmation")
    ],
    events: [],
    links: []
  })
  
  // 6. 构建微服务追踪树
  let microservice_trace_tree = TraceTree({
    root_span: gateway_span,
    child_spans: [
      order_service_span,
      inventory_service_span,
      payment_service_span,
      notification_service_span
    ]
  })
  
  // 7. 验证微服务追踪树
  assert_eq(microservice_trace_tree.root_span.span_id, "span-gateway-entry")
  assert_eq(microservice_trace_tree.child_spans.length(), 4)
  
  // 8. 验证服务调用链
  assert_eq(order_service_span.parent_span_id, Some(gateway_span.span_id))
  assert_eq(inventory_service_span.parent_span_id, Some(order_service_span.span_id))
  assert_eq(payment_service_span.parent_span_id, Some(order_service_span.span_id))
  assert_eq(notification_service_span.parent_span_id, Some(order_service_span.span_id))
  
  // 9. 验证时间顺序
  assert_true(gateway_span.timestamp <= order_service_span.timestamp)
  assert_true(order_service_span.timestamp <= inventory_service_span.timestamp)
  assert_true(order_service_span.timestamp <= payment_service_span.timestamp)
  assert_true(order_service_span.timestamp <= notification_service_span.timestamp)
  
  // 10. 计算微服务调用统计
  let microservice_stats = calculate_microservice_statistics(microservice_trace_tree)
  
  // 11. 验证微服务调用统计
  assert_eq(microservice_stats.total_services, 5)  # gateway + order + inventory + payment + notification
  assert_eq(microservice_stats.service_calls, 4)    # order -> inventory, payment, notification
  assert_eq(microservice_stats.total_duration, 2500)
  assert_eq(microservice_stats.successful_calls, 4)
  assert_eq(microservice_stats.failed_calls, 0)
  
  // 12. 验证服务性能指标
  let gateway_performance = calculate_service_performance(gateway_span)
  let order_performance = calculate_service_performance(order_service_span)
  let inventory_performance = calculate_service_performance(inventory_service_span)
  let payment_performance = calculate_service_performance(payment_service_span)
  let notification_performance = calculate_service_performance(notification_service_span)
  
  assert_eq(gateway_performance.duration, 2500)
  assert_eq(order_performance.duration, 1800)
  assert_eq(inventory_performance.duration, 300)
  assert_eq(payment_performance.duration, 800)
  assert_eq(notification_performance.duration, 400)
  
  // 13. 验证服务依赖关系
  let service_dependencies = analyze_service_dependencies(microservice_trace_tree)
  
  assert_eq(service_dependencies.get("api-gateway"), Some(["order-service"]))
  assert_eq(service_dependencies.get("order-service"), Some(["inventory-service", "payment-service", "notification-service"]))
  assert_eq(service_dependencies.get("inventory-service"), Some([]))
  assert_eq(service_dependencies.get("payment-service"), Some([]))
  assert_eq(service_dependencies.get("notification-service"), Some([]))
}

// 测试5: 错误传播和恢复追踪
test "错误传播和恢复追踪测试" {
  // 1. 创建正常操作的根span
  let normal_root_span = Span({
    trace_id: "trace-error123456",
    span_id: "span-normal-root",
    parent_span_id: None,
    operation_name: "normal.operation.root",
    span_kind: Server,
    timestamp: 1640995200000,
    duration: None,
    status: Ok,
    attributes: [
      ("operation.type", "user.request"),
      ("user.id", "user-12345"),
      ("request.id", "req-67890")
    ],
    events: [],
    links: []
  })
  
  // 2. 创建第一个失败的子span
  let failed_child_span = Span({
    trace_id: normal_root_span.trace_id,
    span_id: "span-failed-child",
    parent_span_id: Some(normal_root_span.span_id),
    operation_name: "database.query",
    span_kind: Client,
    timestamp: normal_root_span.timestamp + 100,
    duration: Some(500),
    status: Error,
    attributes: [
      ("operation.type", "database.query"),
      ("db.statement", "SELECT * FROM users WHERE id = ?"),
      ("db.type", "postgresql")
    ],
    events: [
      SpanEvent({
        timestamp: normal_root_span.timestamp + 200,
        name: "error",
        attributes: [
          ("error.type", "database.connection.error"),
          ("error.message", "Connection timeout"),
          ("error.stack", "DatabaseConnectionException: Connection timeout")
        ]
      })
    ],
    links: []
  })
  
  // 3. 创建重试span
  let retry_span = Span({
    trace_id: normal_root_span.trace_id,
    span_id: "span-retry-attempt",
    parent_span_id: Some(normal_root_span.span_id),
    operation_name: "database.query.retry",
    span_kind: Client,
    timestamp: normal_root_span.timestamp + 700,
    duration: Some(300),
    status: Ok,
    attributes: [
      ("operation.type", "database.query.retry"),
      ("retry.attempt", "1"),
      ("retry.max.attempts", "3"),
      ("db.statement", "SELECT * FROM users WHERE id = ?"),
      ("db.type", "postgresql"),
      ("db.connection.pool", "primary")
    ],
    events: [
      SpanEvent({
        timestamp: normal_root_span.timestamp + 800,
        name: "retry.started",
        attributes: [("retry.reason", "connection.timeout")]
      }),
      SpanEvent({
        timestamp: normal_root_span.timestamp + 1000,
        name: "retry.completed",
        attributes: [("retry.result", "success")]
      })
    ],
    links: [
      SpanLink({
        trace_id: normal_root_span.trace_id,
        span_id: failed_child_span.span_id,
        trace_state: "",
        attributes: [
          ("link.type", "retry.of"),
          ("link.reason", "database.error")
        ]
      })
    ]
  })
  
  // 4. 创建降级操作span
  let fallback_span = Span({
    trace_id: normal_root_span.trace_id,
    span_id: "span-fallback-operation",
    parent_span_id: Some(normal_root_span.span_id),
    operation_name: "cache.fallback.query",
    span_kind: Internal,
    timestamp: normal_root_span.timestamp + 1100,
    duration: Some(200),
    status: Ok,
    attributes: [
      ("operation.type", "cache.fallback"),
      ("fallback.reason", "database.unavailable"),
      ("cache.key", "user:12345"),
      ("cache.hit", "true"),
      ("cache.ttl", "300")
    ],
    events: [
      SpanEvent({
        timestamp: normal_root_span.timestamp + 1200,
        name: "fallback.activated",
        attributes: [("fallback.trigger", "circuit.breaker.open")]
      }),
      SpanEvent({
        timestamp: normal_root_span.timestamp + 1300,
        name: "fallback.completed",
        attributes: [("fallback.result", "success")]
      })
    ],
    links: []
  })
  
  // 5. 完成根span（包含错误恢复信息）
  let completed_root_span = { normal_root_span |
    duration: Some(1500),
    status: Ok,
    events: [
      SpanEvent({
        timestamp: normal_root_span.timestamp + 100,
        name: "operation.started",
        attributes: []
      }),
      SpanEvent({
        timestamp: normal_root_span.timestamp + 700,
        name: "error.detected",
        attributes: [
          ("error.type", "database.connection.error"),
          ("error.recovery.strategy", "retry.with.fallback")
        ]
      }),
      SpanEvent({
        timestamp: normal_root_span.timestamp + 1400,
        name: "recovery.completed",
        attributes: [
          ("recovery.strategy", "retry.and.fallback"),
          ("recovery.result", "success")
        ]
      }),
      SpanEvent({
        timestamp: normal_root_span.timestamp + 1500,
        name: "operation.completed",
        attributes: [
          ("final.result", "success"),
          ("errors.encountered", "1"),
          ("recovery.attempts", "2")
        ]
      })
    ]
  }
  
  // 6. 构建错误恢复追踪树
  let error_recovery_trace_tree = TraceTree({
    root_span: completed_root_span,
    child_spans: [failed_child_span, retry_span, fallback_span]
  })
  
  // 7. 验证错误恢复追踪树
  assert_eq(error_recovery_trace_tree.root_span.span_id, "span-normal-root")
  assert_eq(error_recovery_trace_tree.child_spans.length(), 3)
  
  // 8. 验证错误传播
  assert_eq(failed_child_span.status, Error)
  assert_eq(retry_span.status, Ok)
  assert_eq(fallback_span.status, Ok)
  assert_eq(completed_root_span.status, Ok)
  
  // 9. 验证重试链接
  assert_eq(retry_span.links.length(), 1)
  let retry_link = retry_span.links[0]
  assert_eq(retry_link.trace_id, normal_root_span.trace_id)
  assert_eq(retry_link.span_id, failed_child_span.span_id)
  assert_true(retry_link.attributes.contains fn(attr) { 
    attr.0 == "link.type" && attr.1 == "retry.of" 
  })
  
  // 10. 计算错误恢复统计
  let error_recovery_stats = calculate_error_recovery_statistics(error_recovery_trace_tree)
  
  // 11. 验证错误恢复统计
  assert_eq(error_recovery_stats.total_spans, 4)
  assert_eq(error_recovery_stats.failed_operations, 1)
  assert_eq(error_recovery_stats.recovered_operations, 2)
  assert_eq(error_recovery_stats.retry_attempts, 1)
  assert_eq(error_recovery_stats.fallback_activations, 1)
  assert_eq(error_recovery_stats.final_status, "success")
  assert_eq(error_recovery_stats.error_recovery_rate, 1.0)  # 2 recovered / 2 recovery attempts
  
  // 12. 验证时间线
  assert_true(failed_child_span.timestamp < retry_span.timestamp)
  assert_true(retry_span.timestamp < fallback_span.timestamp)
  assert_true(fallback_span.timestamp <= completed_root_span.timestamp + completed_root_span.duration.unwrap())
  
  // 13. 验证错误处理策略
  let error_handling_strategies = analyze_error_handling_strategies(error_recovery_trace_tree)
  
  assert_true(error_handling_strategies.contains("retry"))
  assert_true(error_handling_strategies.contains("fallback"))
  assert_true(error_handling_strategies.contains("circuit.breaker"))
  
  // 14. 验证错误上下文传播
  for span in [completed_root_span, retry_span, fallback_span] {
    assert_true(span.events.any fn(event) { 
      event.name.contains("error") || event.name.contains("recovery") || event.name.contains("fallback")
    })
  }
}

// 辅助函数：创建子span
fn create_child_span(parent_span : TraceContext, operation_name : String, attributes : Array<(String, String)>) -> TraceContext {
  TraceContext({
    trace_id: parent_span.trace_id,
    span_id: "span-" + Int.random().to_string(),
    parent_span_id: Some(parent_span.span_id),
    sampled: parent_span.sampled,
    baggage: parent_span.baggage,
    trace_flags: parent_span.trace_flags,
    operation_name: operation_name,
    attributes: attributes,
    timestamp: parent_span.timestamp + 100
  })
}

// 辅助函数：计算异步操作统计
fn calculate_async_statistics(trace_tree : TraceTree) -> AsyncStatistics {
  let all_spans = [trace_tree.root_span].concat(trace_tree.child_spans)
  let child_spans = trace_tree.child_spans
  
  let successful_operations = all_spans.count fn(span) { span.status == Ok }
  let failed_operations = all_spans.count fn(span) { span.status == Error }
  
  let child_durations = child_spans.filter_map fn(span) { span.duration }
  let average_child_duration = if child_durations.length() > 0 {
    child_durations.reduce fn(acc, duration) { acc + duration }, 0) / child_durations.length()
  } else { 0 }
  
  AsyncStatistics({
    total_spans: all_spans.length(),
    root_span_duration: trace_tree.root_span.duration.unwrap_or(0),
    successful_operations: successful_operations,
    failed_operations: failed_operations,
    average_child_duration: average_child_duration
  })
}

// 辅助函数：计算批处理统计
fn calculate_batch_statistics(trace_tree : TraceTree) -> BatchStatistics {
  let all_spans = [trace_tree.root_span].concat(trace_tree.child_spans)
  let item_spans = trace_tree.child_spans.filter fn(span) { 
    span.operation_name == "batch.item.process" 
  }
  let phase_spans = trace_tree.child_spans.filter fn(span) { 
    span.operation_name.starts_with("batch.phase.") 
  }
  
  let successful_items = item_spans.count fn(span) { span.status == Ok }
  let failed_items = item_spans.count fn(span) { span.status == Error }
  
  BatchStatistics({
    total_spans: all_spans.length(),
    total_items: item_spans.length(),
    successful_items: successful_items,
    failed_items: failed_items,
    total_duration: trace_tree.root_span.duration.unwrap_or(0),
    phases_completed: phase_spans.length()
  })
}

// 辅助函数：计算微服务统计
fn calculate_microservice_statistics(trace_tree : TraceTree) -> MicroserviceStatistics {
  let all_spans = [trace_tree.root_span].concat(trace_tree.child_spans)
  let services = all_spans.map fn(span) { 
    span.attributes.find fn(attr) { attr.0 == "service.name" }.map fn(attr) { attr.1 }
  }.filter_map fn(service) { service }.unique()
  
  let successful_calls = trace_tree.child_spans.count fn(span) { span.status == Ok }
  let failed_calls = trace_tree.child_spans.count fn(span) { span.status == Error }
  
  MicroserviceStatistics({
    total_services: services.length(),
    service_calls: trace_tree.child_spans.length(),
    total_duration: trace_tree.root_span.duration.unwrap_or(0),
    successful_calls: successful_calls,
    failed_calls: failed_calls
  })
}

// 辅助函数：计算服务性能
fn calculate_service_performance(span : Span) -> ServicePerformance {
  ServicePerformance({
    service_name: span.attributes.find fn(attr) { attr.0 == "service.name" }.map fn(attr) { attr.1 }.unwrap_or("unknown"),
    operation_name: span.operation_name,
    duration: span.duration.unwrap_or(0),
    status: span.status,
    events_count: span.events.length()
  })
}

// 辅助函数：分析服务依赖关系
fn analyze_service_dependencies(trace_tree : TraceTree) -> Map<String, Array<String>> {
  let dependencies = Map.new()
  
  for span in trace_tree.child_spans {
    let service_name = span.attributes.find fn(attr) { attr.0 == "service.name" }.map fn(attr) { attr.1 }.unwrap_or("unknown")
    
    match span.parent_span_id {
      Some(parent_id) => {
        let parent_span = trace_tree.child_spans.find fn(s) { s.span_id == parent_id }
        match parent_span {
          Some(parent) => {
            let parent_service = parent.attributes.find fn(attr) { attr.0 == "service.name" }.map fn(attr) { attr.1 }.unwrap_or("unknown")
            let current_deps = dependencies.get_or(parent_service, [])
            dependencies.set(parent_service, current_deps.concat([service_name]))
          }
          None => {
            // Parent is root span
            let root_service = trace_tree.root_span.attributes.find fn(attr) { attr.0 == "service.name" }.map fn(attr) { attr.1 }.unwrap_or("unknown")
            let current_deps = dependencies.get_or(root_service, [])
            dependencies.set(root_service, current_deps.concat([service_name]))
          }
        }
      }
      None => {}  // No parent
    }
  }
  
  dependencies
}

// 辅助函数：计算错误恢复统计
fn calculate_error_recovery_statistics(trace_tree : TraceTree) -> ErrorRecoveryStatistics {
  let all_spans = [trace_tree.root_span].concat(trace_tree.child_spans)
  
  let failed_operations = all_spans.count fn(span) { span.status == Error }
  let recovered_operations = all_spans.count fn(span) { 
    span.status == Ok && span.events.any fn(event) { event.name.contains("recovery") }
  }
  
  let retry_attempts = all_spans.count fn(span) { 
    span.attributes.any fn(attr) { attr.0 == "retry.attempt" }
  }
  
  let fallback_activations = all_spans.count fn(span) { 
    span.operation_name.contains("fallback") || span.attributes.any fn(attr) { attr.0 == "fallback.reason" }
  }
  
  let final_status = if trace_tree.root_span.status == Ok { "success" } else { "failure" }
  let error_recovery_rate = if (failed_operations + recovered_operations) > 0 {
    recovered_operations.to_float() / (failed_operations + recovered_operations).to_float()
  } else { 0.0 }
  
  ErrorRecoveryStatistics({
    total_spans: all_spans.length(),
    failed_operations: failed_operations,
    recovered_operations: recovered_operations,
    retry_attempts: retry_attempts,
    fallback_activations: fallback_activations,
    final_status: final_status,
    error_recovery_rate: error_recovery_rate
  })
}

// 辅助函数：分析错误处理策略
fn analyze_error_handling_strategies(trace_tree : TraceTree) -> Array<String> {
  let all_spans = [trace_tree.root_span].concat(trace_tree.child_spans)
  let strategies = []
  
  for span in all_spans {
    if span.operation_name.contains("retry") || span.attributes.any fn(attr) { attr.0 == "retry.attempt" } {
      strategies.push("retry")
    }
    
    if span.operation_name.contains("fallback") || span.attributes.any fn(attr) { attr.0 == "fallback.reason" } {
      strategies.push("fallback")
    }
    
    if span.events.any fn(event) { event.name.contains("circuit.breaker") } {
      strategies.push("circuit.breaker")
    }
    
    if span.attributes.any fn(attr) { attr.0 == "timeout.handling" } {
      strategies.push("timeout.handling")
    }
  }
  
  strategies.unique()
}

// 数据类型定义
type TraceContext {
  trace_id : String
  span_id : String
  parent_span_id : Option<String>
  sampled : Bool
  baggage : Array<(String, String)>
  trace_flags : Int
  operation_name : String
  attributes : Array<(String, String)>
  timestamp : Int
}

type Span {
  trace_id : String
  span_id : String
  parent_span_id : Option<String>
  operation_name : String
  span_kind : SpanKind
  timestamp : Int
  duration : Option<Int>
  status : SpanStatus
  attributes : Array<(String, String)>
  events : Array<SpanEvent>
  links : Array<SpanLink>
}

type SpanEvent {
  timestamp : Int
  name : String
  attributes : Array<(String, String)>
}

type SpanLink {
  trace_id : String
  span_id : String
  trace_state : String
  attributes : Array<(String, String)>
}

type SpanKind {
  Server
  Client
  Producer
  Consumer
  Internal
}

type SpanStatus {
  Ok
  Error
}

type TraceTree {
  root_span : Span
  child_spans : Array<Span>
}

type AsyncStatistics {
  total_spans : Int
  root_span_duration : Int
  successful_operations : Int
  failed_operations : Int
  average_child_duration : Int
}

type BatchStatistics {
  total_spans : Int
  total_items : Int
  successful_items : Int
  failed_items : Int
  total_duration : Int
  phases_completed : Int
}

type MicroserviceStatistics {
  total_services : Int
  service_calls : Int
  total_duration : Int
  successful_calls : Int
  failed_calls : Int
}

type ServicePerformance {
  service_name : String
  operation_name : String
  duration : Int
  status : SpanStatus
  events_count : Int
}

type ErrorRecoveryStatistics {
  total_spans : Int
  failed_operations : Int
  recovered_operations : Int
  retry_attempts : Int
  fallback_activations : Int
  final_status : String
  error_recovery_rate : Float
}

type TelemetryPoint {
  timestamp : Int
  metric_name : String
  value : Float
  tags : Array<(String, String)>
}

type AggregatedMetric {
  average : Float
  maximum : Float
  minimum : Float
  count : Int
}

type StreamEvent {
  event_id : String
  timestamp : Int
  event_type : String
  payload : EventPayload
}

type EventPayload {
  RequestStart(RequestStartData)
  RequestComplete(RequestCompleteData)
  ErrorOccurred(ErrorOccurredData)
  SystemMetric(SystemMetricData)
}

type RequestStartData {
  request_id : String
  endpoint : String
}

type RequestCompleteData {
  request_id : String
  duration_ms : Int
}

type ErrorOccurredData {
  error_code : String
  message : String
}

type SystemMetricData {
  metric_name : String
  value : Float
}

type TelemetryRecord {
  timestamp : Int
  service_name : String
  operation_name : String
  duration_ms : Int
  status_code : Int
}

type CompressedRecord {
  timestamp : Int
  service_id : Int
  operation_id : Int
  duration_ms : Int
  status_code : Int
}

type DataPoint {
  id : Int
  value : Int
  category : Int
  timestamp : Int
  metadata : Array<(String, String)>
}

type CategoryResult {
  category : Int
  count : Int
  sum : Int
  average : Float
  max : Int
  min : Int
}

type MultiDimensionalTelemetry {
  id : String
  timestamp : Int
  service : String
  environment : String
  region : String
  metric_type : String
  value : Float
  tags : Array<(String, String)>
}