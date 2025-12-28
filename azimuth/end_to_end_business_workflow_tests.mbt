// End-to-End Business Process Test Suite for Azimuth Telemetry System
// Test cases covering complete business workflows and real-world scenarios

test "ecommerce_order_fulfillment_process" {
  // Complete e-commerce order fulfillment workflow
  
  // Initialize telemetry components
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  let meter_provider = MeterProvider::default()
  
  // Create service-specific components
  let frontend_tracer = TracerProvider::get_tracer(tracer_provider, "frontend.service", Some("1.2.0"))
  let order_tracer = TracerProvider::get_tracer(tracer_provider, "order.service", Some("2.1.0"))
  let payment_tracer = TracerProvider::get_tracer(tracer_provider, "payment.service", Some("1.5.2"))
  let inventory_tracer = TracerProvider::get_tracer(tracer_provider, "inventory.service", Some("3.0.1"))
  let shipping_tracer = TracerProvider::get_tracer(tracer_provider, "shipping.service", Some("2.4.0"))
  let notification_tracer = TracerProvider::get_tracer(tracer_provider, "notification.service", Some("1.8.0"))
  
  let frontend_logger = LoggerProvider::get_logger(logger_provider, "frontend.service")
  let order_logger = LoggerProvider::get_logger(logger_provider, "order.service")
  let payment_logger = LoggerProvider::get_logger(logger_provider, "payment.service")
  let inventory_logger = LoggerProvider::get_logger(logger_provider, "inventory.service")
  let shipping_logger = LoggerProvider::get_logger(logger_provider, "shipping.service")
  let notification_logger = LoggerProvider::get_logger(logger_provider, "notification.service")
  
  let metrics_meter = MeterProvider::get_meter(meter_provider, "ecommerce.metrics")
  
  // Create business metrics
  let order_counter = Meter::create_counter(metrics_meter, "orders.total", Some("Total orders"), Some("orders"))
  let revenue_counter = Meter::create_counter(metrics_meter, "revenue.total", Some("Total revenue"), Some("currency"))
  let payment_histogram = Meter::create_histogram(metrics_meter, "payment.processing.time", Some("Payment processing time"), Some("ms"))
  let shipping_gauge = Meter::create_gauge(metrics_meter, "pending.shipments", Some("Pending shipments"), Some("shipments"))
  
  // Step 1: Customer browsing and cart management
  let frontend_span = Tracer::start_span(frontend_tracer, "customer.browsing")
  
  Span::add_event(frontend_span, "page.viewed", Some([
    ("page.type", StringValue("product.list")),
    ("category", StringValue("electronics")),
    ("user.id", StringValue("customer_12345"))
  ]))
  
  Span::add_event(frontend_span, "product.viewed", Some([
    ("product.id", StringValue("PROD-001")),
    ("product.name", StringValue("Smartphone X")),
    ("price", StringValue("699.99"))
  ]))
  
  Span::add_event(frontend_span, "product.added.to.cart", Some([
    ("product.id", StringValue("PROD-001")),
    ("quantity", IntValue(1)),
    ("cart.total", StringValue("699.99"))
  ]))
  
  let browsing_log = LogRecord::new(Info, "Customer added product PROD-001 to cart")
  Logger::emit(frontend_logger, browsing_log)
  
  Span::end(frontend_span)
  
  // Step 2: Order creation
  let order_span = Tracer::start_span(order_tracer, "order.creation")
  let order_context = Span::span_context(order_span)
  
  let order_id = "ORDER-2025-001"
  let customer_id = "customer_12345"
  let order_total = 699.99
  
  Span::add_event(order_span, "order.started", Some([
    ("order.id", StringValue(order_id)),
    ("customer.id", StringValue(customer_id)),
    ("order.total", StringValue(order_total.to_string()))
  ]))
  
  // Record order metrics
  let order_attrs = Attributes::new()
  Attributes::set(order_attrs, "customer.id", StringValue(customer_id))
  Attributes::set(order_attrs, "order.amount", StringValue(order_total.to_string()))
  Counter::add(order_counter, 1.0, Some(order_attrs))
  
  let order_creation_log = LogRecord::new_with_context(
    Info,
    Some("Order " + order_id + " created for customer " + customer_id),
    Some(order_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(order_context)),
    Some(SpanContext::span_id(order_context)),
    None
  )
  Logger::emit(order_logger, order_creation_log)
  
  Span::end(order_span)
  
  // Step 3: Payment processing
  let payment_span = Tracer::start_span(payment_tracer, "payment.processing")
  let payment_context = Span::span_context(payment_span)
  
  let payment_start = Clock::now_unix_nanos(Clock::system())
  
  Span::add_event(payment_span, "payment.started", Some([
    ("order.id", StringValue(order_id)),
    ("payment.method", StringValue("credit_card")),
    ("amount", StringValue(order_total.to_string())),
    ("currency", StringValue("USD"))
  ]))
  
  // Simulate payment processing
  Span::add_event(payment_span, "payment.validation", Some([
    ("card.number.last4", StringValue("1234")),
    ("card.type", StringValue("visa")),
    ("validation.result", StringValue("passed"))
  ]))
  
  Span::add_event(payment_span, "payment.gateway.call", Some([
    ("gateway", StringValue("stripe")),
    ("transaction.id", StringValue("txn_1234567890"))
  ]))
  
  Span::add_event(payment_span, "payment.completed", Some([
    ("transaction.status", StringValue("succeeded")),
    ("payment.id", StringValue("PAY-123456"))
  ]))
  
  let payment_end = Clock::now_unix_nanos(Clock::system())
  let payment_duration = (payment_end - payment_start).to_double() / 1000000.0 // Convert to ms
  
  // Record payment metrics
  let payment_attrs = Attributes::new()
  Attributes::set(payment_attrs, "payment.method", StringValue("credit_card"))
  Attributes::set(payment_attrs, "payment.gateway", StringValue("stripe"))
  Histogram::record(payment_histogram, payment_duration, Some(payment_attrs))
  
  // Record revenue
  let revenue_attrs = Attributes::new()
  Attributes::set(revenue_attrs, "currency", StringValue("USD"))
  Counter::add(revenue_counter, order_total, Some(revenue_attrs))
  
  let payment_log = LogRecord::new_with_context(
    Info,
    Some("Payment processed successfully for order " + order_id + " in " + payment_duration.to_string() + "ms"),
    Some(payment_attrs),
    Some(payment_start),
    Some(payment_end),
    Some(SpanContext::trace_id(payment_context)),
    Some(SpanContext::span_id(payment_context)),
    None
  )
  Logger::emit(payment_logger, payment_log)
  
  Span::end(payment_span)
  
  // Step 4: Inventory management
  let inventory_span = Tracer::start_span(inventory_tracer, "inventory.allocation")
  let inventory_context = Span::span_context(inventory_span)
  
  Span::add_event(inventory_span, "inventory.reservation.started", Some([
    ("order.id", StringValue(order_id)),
    ("product.id", StringValue("PROD-001")),
    ("quantity.requested", IntValue(1))
  ]))
  
  Span::add_event(inventory_span, "inventory.checked", Some([
    ("product.id", StringValue("PROD-001")),
    ("stock.available", IntValue(50)),
    ("stock.reserved", IntValue(1))
  ]))
  
  Span::add_event(inventory_span, "inventory.reserved", Some([
    ("product.id", StringValue("PROD-001")),
    ("reservation.id", StringValue("RES-123456")),
    ("quantity.reserved", IntValue(1))
  ]))
  
  let inventory_log = LogRecord::new_with_context(
    Info,
    Some("Inventory reserved for order " + order_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(inventory_context)),
    Some(SpanContext::span_id(inventory_context)),
    None
  )
  Logger::emit(inventory_logger, inventory_log)
  
  Span::end(inventory_span)
  
  // Step 5: Shipping arrangement
  let shipping_span = Tracer::start_span(shipping_tracer, "shipping.arrangement")
  let shipping_context = Span::span_context(shipping_span)
  
  Span::add_event(shipping_span, "shipping.started", Some([
    ("order.id", StringValue(order_id)),
    ("shipping.method", StringValue("standard"))
  ]))
  
  Span::add_event(shipping_span, "address.verified", Some([
    ("shipping.address", StringValue("123 Main St, City, ST 12345")),
    ("verification.status", StringValue("verified"))
  ]))
  
  Span::add_event(shipping_span, "carrier.selected", Some([
    ("carrier", StringValue("UPS")),
    ("service.level", StringValue("Ground"))
  ]))
  
  Span::add_event(shipping_span, "label.generated", Some([
    ("tracking.number", StringValue("1Z999AA10123456784")),
    ("label.id", StringValue("LBL-123456"))
  ]))
  
  let shipping_log = LogRecord::new_with_context(
    Info,
    Some("Shipping arranged for order " + order_id + " with tracking 1Z999AA10123456784"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(shipping_context)),
    Some(SpanContext::span_id(shipping_context)),
    None
  )
  Logger::emit(shipping_logger, shipping_log)
  
  Span::end(shipping_span)
  
  // Step 6: Customer notifications
  let notification_span = Tracer::start_span(notification_tracer, "customer.notifications")
  let notification_context = Span::span_context(notification_span)
  
  Span::add_event(notification_span, "order.confirmation.sent", Some([
    ("notification.type", StringValue("email")),
    ("template", StringValue("order_confirmation")),
    ("recipient", StringValue("customer@example.com"))
  ]))
  
  Span::add_event(notification_span, "shipping.notification.sent", Some([
    ("notification.type", StringValue("sms")),
    ("template", StringValue("shipping_update")),
    ("recipient", StringValue("+1-555-123-4567"))
  ]))
  
  Span::add_event(notification_span, "push.notification.sent", Some([
    ("notification.type", StringValue("push")),
    ("device", StringValue("mobile_app")),
    ("message", StringValue("Your order has been shipped!"))
  ]))
  
  let notification_log = LogRecord::new_with_context(
    Info,
    Some("All notifications sent for order " + order_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(notification_context)),
    Some(SpanContext::span_id(notification_context)),
    None
  )
  Logger::emit(notification_logger, notification_log)
  
  Span::end(notification_span)
  
  // Business process completion verification
  assert_eq(order_id, "ORDER-2025-001")
  assert_eq(customer_id, "customer_12345")
  assert_true(order_total > 0.0)
  
  // Verify metrics were created
  assert_eq(order_counter.name, "orders.total")
  assert_eq(revenue_counter.name, "revenue.total")
  assert_eq(payment_histogram.name, "payment.processing.time")
  assert_eq(shipping_gauge.name, "pending.shipments")
}

test "financial_trading_workflow" {
  // Complete financial trading workflow
  
  // Initialize telemetry for trading system
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  let meter_provider = MeterProvider::default()
  
  let gateway_tracer = TracerProvider::get_tracer(tracer_provider, "trading.gateway", Some("3.1.0"))
  let execution_tracer = TracerProvider::get_tracer(tracer_provider, "trade.execution", Some("2.4.1"))
  let risk_tracer = TracerProvider::get_tracer(tracer_provider, "risk.management", Some("1.8.2"))
  let compliance_tracer = TracerProvider::get_tracer(tracer_provider, "compliance.check", Some("2.0.1"))
  let settlement_tracer = TracerProvider::get_tracer(tracer_provider, "trade.settlement", Some("1.5.3"))
  
  let gateway_logger = LoggerProvider::get_logger(logger_provider, "trading.gateway")
  let execution_logger = LoggerProvider::get_logger(logger_provider, "trade.execution")
  let risk_logger = LoggerProvider::get_logger(logger_provider, "risk.management")
  let compliance_logger = LoggerProvider::get_logger(logger_provider, "compliance.check")
  let settlement_logger = LoggerProvider::get_logger(logger_provider, "trade.settlement")
  
  let trading_meter = MeterProvider::get_meter(meter_provider, "trading.metrics")
  
  // Create trading metrics
  let trade_counter = Meter::create_counter(trading_meter, "trades.total", Some("Total trades"), Some("trades"))
  let volume_counter = Meter::create_counter(trading_meter, "trading.volume.total", Some("Total trading volume"), Some("shares"))
  let latency_histogram = Meter::create_histogram(trading_meter, "trade.execution.latency", Some("Trade execution latency"), Some("microseconds"))
  let risk_score_gauge = Meter::create_gauge(trading_meter, "portfolio.risk.score", Some("Portfolio risk score"), Some("score"))
  
  // Step 1: Trade request received
  let gateway_span = Tracer::start_span(gateway_tracer, "trade.request.received")
  let gateway_context = Span::span_context(gateway_span)
  
  let trade_id = "TRADE-2025-98765"
  let client_id = "CLIENT-001"
  let symbol = "AAPL"
  let quantity = 100
  let order_type = "MARKET"
  
  Span::add_event(gateway_span, "trade.request.validated", Some([
    ("trade.id", StringValue(trade_id)),
    ("client.id", StringValue(client_id)),
    ("symbol", StringValue(symbol)),
    ("quantity", IntValue(quantity)),
    ("order.type", StringValue(order_type)),
    ("request.timestamp", StringValue("2025-01-01T09:30:00Z"))
  ]))
  
  let gateway_log = LogRecord::new_with_context(
    Info,
    Some("Trade request received: " + trade_id + " for " + quantity.to_string() + " shares of " + symbol),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(gateway_context)),
    Some(SpanContext::span_id(gateway_context)),
    None
  )
  Logger::emit(gateway_logger, gateway_log)
  
  Span::end(gateway_span)
  
  // Step 2: Risk management check
  let risk_span = Tracer::start_span(risk_tracer, "risk.assessment")
  let risk_context = Span::span_context(risk_span)
  
  Span::add_event(risk_span, "risk.check.started", Some([
    ("trade.id", StringValue(trade_id)),
    ("client.id", StringValue(client_id)),
    ("portfolio.value", StringValue("1000000.00"))
  ]))
  
  Span::add_event(risk_span, "position.limits.checked", Some([
    ("symbol", StringValue(symbol)),
    ("current.position", IntValue(500)),
    ("limit", IntValue(1000)),
    ("utilization", StringValue("50.0%"))
  ]))
  
  Span::add_event(risk_span, "margin.requirements.calculated", Some([
    ("required.margin", StringValue("70000.00")),
    ("available.margin", StringValue("200000.00")),
    ("margin.ratio", StringValue("35.0%"))
  ]))
  
  Span::add_event(risk_span, "risk.score.calculated", Some([
    ("risk.score", FloatValue(3.2)),
    ("risk.level", StringValue("MEDIUM")),
    ("approval.status", StringValue("AUTO_APPROVED"))
  ]))
  
  let risk_log = LogRecord::new_with_context(
    Info,
    Some("Risk assessment completed for " + trade_id + " - RISK SCORE: 3.2 (MEDIUM)"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(risk_context)),
    Some(SpanContext::span_id(risk_context)),
    None
  )
  Logger::emit(risk_logger, risk_log)
  
  Span::end(risk_span)
  
  // Step 3: Compliance check
  let compliance_span = Tracer::start_span(compliance_tracer, "compliance.validation")
  let compliance_context = Span::span_context(compliance_span)
  
  Span::add_event(compliance_span, "compliance.check.started", Some([
    ("trade.id", StringValue(trade_id)),
    ("client.profile", StringValue("RETAIL")),
    ("jurisdiction", StringValue("US"))
  ]))
  
  Span::add_event(compliance_span, "insider.trading.checked", Some([
    ("symbol", StringValue(symbol)),
    ("blacklist.status", StringValue("CLEAR")),
    ("restricted.period", StringValue("NONE"))
  ]))
  
  Span::add_event(compliance_span, "market.manipulation.checked", Some([
    ("order.pattern", StringValue("NORMAL")),
    ("wash.sale.risk", StringValue("LOW")),
    ("spoofing.indicator", StringValue("NONE"))
  ]))
  
  Span::add_event(compliance_span, "compliance.approved", Some([
    ("compliance.status", StringValue("APPROVED")),
    ("regulatory.flags", IntValue(0)),
    ("approval.timestamp", StringValue("2025-01-01T09:30:01Z"))
  ]))
  
  let compliance_log = LogRecord::new_with_context(
    Info,
    Some("Compliance check passed for " + trade_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(compliance_context)),
    Some(SpanContext::span_id(compliance_context)),
    None
  )
  Logger::emit(compliance_logger, compliance_log)
  
  Span::end(compliance_span)
  
  // Step 4: Trade execution
  let execution_span = Tracer::start_span(execution_tracer, "trade.execution")
  let execution_context = Span::span_context(execution_span)
  
  let execution_start = Clock::now_unix_nanos(Clock::system())
  
  Span::add_event(execution_span, "order.routed", Some([
    ("trade.id", StringValue(trade_id)),
    ("destination", StringValue("NASDAQ")),
    ("routing.algorithm", StringValue("SMART_ORDER_ROUTER"))
  ]))
  
  Span::add_event(execution_span, "order.submitted", Some([
    ("exchange.order.id", StringValue("EXCH-123456")),
    ("submitted.price", StringValue("175.25")),
    ("submitted.quantity", IntValue(quantity))
  ]))
  
  Span::add_event(execution_span, "partial.fills", Some([
    ("fill.1.quantity", IntValue(60)),
    ("fill.1.price", StringValue("175.24")),
    ("fill.1.timestamp", StringValue("2025-01-01T09:30:02Z"))
  ]))
  
  Span::add_event(execution_span, "order.completed", Some([
    ("fill.2.quantity", IntValue(40)),
    ("fill.2.price", StringValue("175.26")),
    ("fill.2.timestamp", StringValue("2025-01-01T09:30:03Z")),
    ("average.price", StringValue("175.25")),
    ("total.quantity", IntValue(quantity))
  ]))
  
  let execution_end = Clock::now_unix_nanos(Clock::system())
  let execution_latency = execution_end - execution_start
  
  // Record trading metrics
  let trade_attrs = Attributes::new()
  Attributes::set(trade_attrs, "symbol", StringValue(symbol))
  Attributes::set(trade_attrs, "order.type", StringValue(order_type))
  Attributes::set(trade_attrs, "client.id", StringValue(client_id))
  Counter::add(trade_counter, 1.0, Some(trade_attrs))
  
  let volume_attrs = Attributes::new()
  Attributes::set(volume_attrs, "symbol", StringValue(symbol))
  Counter::add(volume_counter, quantity.to_double(), Some(volume_attrs))
  
  let latency_attrs = Attributes::new()
  Attributes::set(latency_attrs, "symbol", StringValue(symbol))
  Attributes::set(latency_attrs, "order.type", StringValue(order_type))
  Histogram::record(latency_histogram, execution_latency.to_double(), Some(latency_attrs))
  
  let execution_log = LogRecord::new_with_context(
    Info,
    Some("Trade executed: " + trade_id + " - " + quantity.to_string() + " shares of " + symbol + " at $175.25"),
    Some(Attributes::new()),
    Some(execution_start),
    Some(execution_end),
    Some(SpanContext::trace_id(execution_context)),
    Some(SpanContext::span_id(execution_context)),
    None
  )
  Logger::emit(execution_logger, execution_log)
  
  Span::end(execution_span)
  
  // Step 5: Trade settlement
  let settlement_span = Tracer::start_span(settlement_tracer, "trade.settlement")
  let settlement_context = Span::span_context(settlement_span)
  
  Span::add_event(settlement_span, "clearing.started", Some([
    ("trade.id", StringValue(trade_id)),
    ("clearing.house", StringValue("DTCC")),
    ("settlement.date", StringValue("2025-01-03"))
  ]))
  
  Span::add_event(settlement_span, "funds.transfer.initiated", Some([
    ("transfer.amount", StringValue("17525.00")),
    ("debit.account", StringValue("CLIENT-001-CASH")),
    ("credit.account", StringValue("BROKER-CASH"))
  ]))
  
  Span::add_event(settlement_span, "securities.transfer.initiated", Some([
    ("security.quantity", IntValue(quantity)),
    ("from.account", StringValue("BROKER-SECURITIES")),
    ("to.account", StringValue("CLIENT-001-SECURITIES"))
  ]))
  
  Span::add_event(settlement_span, "settlement.completed", Some([
    ("settlement.id", StringValue("SETT-123456")),
    ("completion.timestamp", StringValue("2025-01-03T16:00:00Z"))
  ]))
  
  let settlement_log = LogRecord::new_with_context(
    Info,
    Some("Trade settlement completed for " + trade_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(settlement_context)),
    Some(SpanContext::span_id(settlement_context)),
    None
  )
  Logger::emit(settlement_logger, settlement_log)
  
  Span::end(settlement_span)
  
  // Verify trading workflow
  assert_eq(trade_id, "TRADE-2025-98765")
  assert_eq(symbol, "AAPL")
  assert_eq(quantity, 100)
  assert_true(execution_latency > 0L)
  
  // Verify trading metrics
  assert_eq(trade_counter.name, "trades.total")
  assert_eq(volume_counter.name, "trading.volume.total")
  assert_eq(latency_histogram.name, "trade.execution.latency")
  assert_eq(risk_score_gauge.name, "portfolio.risk.score")
}

test "healthcare_patient_journey_workflow" {
  // Complete healthcare patient journey workflow
  
  // Initialize healthcare telemetry
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  let meter_provider = MeterProvider::default()
  
  let registration_tracer = TracerProvider::get_tracer(tracer_provider, "patient.registration", Some("2.1.0"))
  let clinical_tracer = TracerProvider::get_tracer(tracer_provider, "clinical.system", Some("3.2.1"))
  let lab_tracer = TracerProvider::get_tracer(tracer_provider, "laboratory.system", Some("1.8.0"))
  let pharmacy_tracer = TracerProvider::get_tracer(tracer_provider, "pharmacy.system", Some("2.0.1"))
  let billing_tracer = TracerProvider::get_tracer(tracer_provider, "billing.system", Some("1.5.2"))
  
  let registration_logger = LoggerProvider::get_logger(logger_provider, "patient.registration")
  let clinical_logger = LoggerProvider::get_logger(logger_provider, "clinical.system")
  let lab_logger = LoggerProvider::get_logger(logger_provider, "laboratory.system")
  let pharmacy_logger = LoggerProvider::get_logger(logger_provider, "pharmacy.system")
  let billing_logger = LoggerProvider::get_logger(logger_provider, "billing.system")
  
  let healthcare_meter = MeterProvider::get_meter(meter_provider, "healthcare.metrics")
  
  // Create healthcare metrics
  let patient_counter = Meter::create_counter(healthcare_meter, "patients.total", Some("Total patients"), Some("patients"))
  let encounter_counter = Meter::create_counter(healthcare_meter, "patient.encounters", Some("Patient encounters"), Some("encounters"))
  let lab_test_histogram = Meter::create_histogram(healthcare_meter, "lab.test.turnaround", Some("Lab test turnaround time"), Some("hours"))
  let wait_time_gauge = Meter::create_gauge(healthcare_meter, "patient.wait.time", Some("Patient wait time"), Some("minutes"))
  
  // Step 1: Patient registration
  let registration_span = Tracer::start_span(registration_tracer, "patient.registration")
  let registration_context = Span::span_context(registration_span)
  
  let patient_id = "PAT-2025-001"
  let mrn = "MRN-123456789"
  
  Span::add_event(registration_span, "patient.registered", Some([
    ("patient.id", StringValue(patient_id)),
    ("mrn", StringValue(mrn)),
    ("patient.name", StringValue("John Doe")),
    ("date.of.birth", StringValue("1980-01-15")),
    ("registration.timestamp", StringValue("2025-01-01T08:00:00Z"))
  ]))
  
  Span::add_event(registration_span, "insurance.verified", Some([
    ("insurance.provider", StringValue("HealthPlus")),
    ("policy.number", StringValue("HP-987654321")),
    ("coverage.status", StringValue("ACTIVE"))
  ]))
  
  let registration_log = LogRecord::new_with_context(
    Info,
    Some("Patient registered: " + patient_id + " (MRN: " + mrn + ")"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(registration_context)),
    Some(SpanContext::span_id(registration_context)),
    None
  )
  Logger::emit(registration_logger, registration_log)
  
  // Record patient metrics
  let patient_attrs = Attributes::new()
  Attributes::set(patient_attrs, "patient.type", StringValue("new"))
  Attributes::set(patient_attrs, "insurance.type", StringValue("private"))
  Counter::add(patient_counter, 1.0, Some(patient_attrs))
  
  Span::end(registration_span)
  
  // Step 2: Clinical encounter
  let clinical_span = Tracer::start_span(clinical_tracer, "clinical.encounter")
  let clinical_context = Span::span_context(clinical_span)
  
  let encounter_id = "ENC-2025-001"
  let provider_id = "DR-001"
  
  Span::add_event(clinical_span, "encounter.started", Some([
    ("encounter.id", StringValue(encounter_id)),
    ("patient.id", StringValue(patient_id)),
    ("provider.id", StringValue(provider_id)),
    ("encounter.type", StringValue("OFFICE_VISIT"))
  ]))
  
  Span::add_event(clinical_span, "vital.signs.recorded", Some([
    ("blood.pressure", StringValue("120/80")),
    ("heart.rate", IntValue(72)),
    ("temperature", StringValue("98.6F")),
    ("oxygen.saturation", StringValue("98%"))
  ]))
  
  Span::add_event(clinical_span, "symptoms.documented", Some([
    ("chief.complaint", StringValue("Routine checkup")),
    ("additional.symptoms", StringValue("Occasional headache"))
  ]))
  
  Span::add_event(clinical_span, "diagnosis.recorded", Some([
    ("diagnosis.code", StringValue("Z00.00")),
    ("diagnosis.description", StringValue("Encounter for general adult medical examination"))
  ]))
  
  let clinical_log = LogRecord::new_with_context(
    Info,
    Some("Clinical encounter completed: " + encounter_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(clinical_context)),
    Some(SpanContext::span_id(clinical_context)),
    None
  )
  Logger::emit(clinical_logger, clinical_log)
  
  // Record encounter metrics
  let encounter_attrs = Attributes::new()
  Attributes::set(encounter_attrs, "encounter.type", StringValue("OFFICE_VISIT"))
  Attributes::set(encounter_attrs, "provider.specialty", StringValue("GENERAL_PRACTICE"))
  Counter::add(encounter_counter, 1.0, Some(encounter_attrs))
  
  Span::end(clinical_span)
  
  // Step 3: Laboratory testing
  let lab_span = Tracer::start_span(lab_tracer, "laboratory.testing")
  let lab_context = Span::span_context(lab_span)
  
  let lab_start = Clock::now_unix_nanos(Clock::system())
  
  Span::add_event(lab_span, "specimen.collected", Some([
    ("order.id", StringValue("LAB-001")),
    ("patient.id", StringValue(patient_id)),
    ("specimen.type", StringValue("BLOOD")),
    ("collection.timestamp", StringValue("2025-01-01T08:30:00Z"))
  ]))
  
  Span::add_event(lab_span, "tests.ordered", Some([
    ("test.1", StringValue("CBC")),
    ("test.2", StringValue("Comprehensive Metabolic Panel")),
    ("test.3", StringValue("Lipid Panel"))
  ]))
  
  Span::add_event(lab_span, "specimen.received", Some([
    ("lab.timestamp", StringValue("2025-01-01T09:00:00Z")),
    ("accession.number", StringValue("ACC-123456"))
  ]))
  
  Span::add_event(lab_span, "results.available", Some([
    ("result.timestamp", StringValue("2025-01-01T14:00:00Z")),
    ("cbc.status", StringValue("NORMAL")),
    ("cmp.status", StringValue("NORMAL")),
    ("lipid.status", StringValue("BORDERLINE_HIGH"))
  ]))
  
  let lab_end = Clock::now_unix_nanos(Clock::system())
  let lab_turnaround = (lab_end - lab_start).to_double() / 3600000000000.0 // Convert to hours
  
  // Record lab metrics
  let lab_attrs = Attributes::new()
  Attributes::set(lab_attrs, "test.type", StringValue("ROUTINE_PANEL"))
  Histogram::record(lab_test_histogram, lab_turnaround, Some(lab_attrs))
  
  let lab_log = LogRecord::new_with_context(
    Info,
    Some("Lab results available for " + patient_id + " - Turnaround: " + lab_turnaround.to_string() + " hours"),
    Some(Attributes::new()),
    Some(lab_start),
    Some(lab_end),
    Some(SpanContext::trace_id(lab_context)),
    Some(SpanContext::span_id(lab_context)),
    None
  )
  Logger::emit(lab_logger, lab_log)
  
  Span::end(lab_span)
  
  // Step 4: Pharmacy dispensing
  let pharmacy_span = Tracer::start_span(pharmacy_tracer, "pharmacy.dispensing")
  let pharmacy_context = Span::span_context(pharmacy_span)
  
  Span::add_event(pharmacy_span, "prescription.received", Some([
    ("prescription.id", StringValue("RX-001")),
    ("patient.id", StringValue(patient_id)),
    ("prescriber.id", StringValue(provider_id))
  ]))
  
  Span::add_event(pharmacy_span, "medication.ordered", Some([
    ("medication.name", StringValue("Lisinopril")),
    ("dosage", StringValue("10mg")),
    ("quantity", IntValue(30)),
    ("sig", StringValue("Take 1 tablet daily"))
  ]))
  
  Span::add_event(pharmacy_span, "insurance.claim.processed", Some([
    ("claim.status", StringValue("APPROVED")),
    ("copay.amount", StringValue("10.00")),
    ("claim.id", StringValue("CLAIM-123456"))
  ]))
  
  Span::add_event(pharmacy_span, "medication.dispensed", Some([
    ("dispense.timestamp", StringValue("2025-01-01T16:30:00Z")),
    ("dispense.id", StringValue("DISP-001")),
    ("pharmacist.id", StringValue("PHARM-001"))
  ]))
  
  let pharmacy_log = LogRecord::new_with_context(
    Info,
    Some("Medication dispensed for " + patient_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(pharmacy_context)),
    Some(SpanContext::span_id(pharmacy_context)),
    None
  )
  Logger::emit(pharmacy_logger, pharmacy_log)
  
  Span::end(pharmacy_span)
  
  // Step 5: Billing and claims
  let billing_span = Tracer::start_span(billing_tracer, "billing.processing")
  let billing_context = Span::span_context(billing_span)
  
  Span::add_event(billing_span, "claim.generated", Some([
    ("claim.id", StringValue("CLAIM-789012")),
    ("patient.id", StringValue(patient_id)),
    ("encounter.id", StringValue(encounter_id)),
    ("total.charges", StringValue("250.00"))
  ]))
  
  Span::add_event(billing_span, "services.billed", Some([
    ("service.1", StringValue("Office Visit - Level 3")),
    ("service.2", StringValue("Laboratory Panel")),
    ("service.1.charge", StringValue("150.00")),
    ("service.2.charge", StringValue("100.00"))
  ]))
  
  Span::add_event(billing_span, "insurance.submitted", Some([
    ("submission.timestamp", StringValue("2025-01-01T17:00:00Z")),
    ("payer", StringValue("HealthPlus")),
    ("submission.method", StringValue("ELECTRONIC"))
  ]))
  
  Span::add_event(billing_span, "payment.received", Some([
    ("payment.amount", StringValue("200.00")),
    ("patient.responsibility", StringValue("50.00")),
    ("payment.date", StringValue("2025-01-15"))
  ]))
  
  let billing_log = LogRecord::new_with_context(
    Info,
    Some("Billing completed for " + patient_id + " - Total: $250.00"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(billing_context)),
    Some(SpanContext::span_id(billing_context)),
    None
  )
  Logger::emit(billing_logger, billing_log)
  
  Span::end(billing_span)
  
  // Verify healthcare workflow
  assert_eq(patient_id, "PAT-2025-001")
  assert_eq(mrn, "MRN-123456789")
  assert_eq(encounter_id, "ENC-2025-001")
  assert_true(lab_turnaround > 0.0)
  
  // Verify healthcare metrics
  assert_eq(patient_counter.name, "patients.total")
  assert_eq(encounter_counter.name, "patient.encounters")
  assert_eq(lab_test_histogram.name, "lab.test.turnaround.time")
  assert_eq(wait_time_gauge.name, "patient.wait.time")
}

test "iot_device_management_workflow" {
  // Complete IoT device management workflow
  
  // Initialize IoT telemetry
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  let meter_provider = MeterProvider::default()
  
  let provisioning_tracer = TracerProvider::get_tracer(tracer_provider, "device.provisioning", Some("2.3.0"))
  let telemetry_tracer = TracerProvider::get_tracer(tracer_provider, "telemetry.ingestion", Some("3.1.2"))
  let monitoring_tracer = TracerProvider::get_tracer(tracer_provider, "device.monitoring", Some("2.0.1"))
  let analytics_tracer = TracerProvider::get_tracer(tracer_provider, "telemetry.analytics", Some("1.8.0"))
  let maintenance_tracer = TracerProvider::get_tracer(tracer_provider, "device.maintenance", Some("1.5.1"))
  
  let provisioning_logger = LoggerProvider::get_logger(logger_provider, "device.provisioning")
  let telemetry_logger = LoggerProvider::get_logger(logger_provider, "telemetry.ingestion")
  let monitoring_logger = LoggerProvider::get_logger(logger_provider, "device.monitoring")
  let analytics_logger = LoggerProvider::get_logger(logger_provider, "telemetry.analytics")
  let maintenance_logger = LoggerProvider::get_logger(logger_provider, "device.maintenance")
  
  let iot_meter = MeterProvider::get_meter(meter_provider, "iot.metrics")
  
  // Create IoT metrics
  let device_counter = Meter::create_counter(iot_meter, "devices.total", Some("Total devices"), Some("devices"))
  let telemetry_counter = Meter::create_counter(iot_meter, "telemetry.messages", Some("Telemetry messages"), Some("messages"))
  let latency_histogram = Meter::create_histogram(iot_meter, "telemetry.latency", Some("Telemetry latency"), Some("milliseconds"))
  let device_health_gauge = Meter::create_gauge(iot_meter, "device.health.score", Some("Device health score"), Some("score"))
  
  // Step 1: Device provisioning
  let provisioning_span = Tracer::start_span(provisioning_tracer, "device.provisioning")
  let provisioning_context = Span::span_context(provisioning_span)
  
  let device_id = "IOT-DEVICE-001"
  let device_type = "TEMPERATURE_SENSOR"
  
  Span::add_event(provisioning_span, "device.registered", Some([
    ("device.id", StringValue(device_id)),
    ("device.type", StringValue(device_type)),
    ("manufacturer", StringValue("SensorTech")),
    ("model", StringValue("ST-TEMP-1000")),
    ("firmware.version", StringValue("1.2.3"))
  ]))
  
  Span::add_event(provisioning_span, "certificates.issued", Some([
    ("device.certificate", StringValue("CERT-123456")),
    ("certificate.expiry", StringValue("2026-01-01")),
    ("ca.authority", StringValue("IoT-Root-CA"))
  ]))
  
  Span::add_event(provisioning_span, "configuration.applied", Some([
    ("sampling.rate", StringValue("60s")),
    ("reporting.interval", StringValue("300s")),
    ("threshold.critical", StringValue("80C")),
    ("threshold.warning", StringValue("70C"))
  ]))
  
  let provisioning_log = LogRecord::new_with_context(
    Info,
    Some("Device provisioned: " + device_id + " (" + device_type + ")"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(provisioning_context)),
    Some(SpanContext::span_id(provisioning_context)),
    None
  )
  Logger::emit(provisioning_logger, provisioning_log)
  
  // Record device metrics
  let device_attrs = Attributes::new()
  Attributes::set(device_attrs, "device.type", StringValue(device_type))
  Attributes::set(device_attrs, "manufacturer", StringValue("SensorTech"))
  Counter::add(device_counter, 1.0, Some(device_attrs))
  
  Span::end(provisioning_span)
  
  // Step 2: Telemetry ingestion
  let telemetry_span = Tracer::start_span(telemetry_tracer, "telemetry.ingestion")
  let telemetry_context = Span::span_context(telemetry_span)
  
  let telemetry_start = Clock::now_unix_nanos(Clock::system())
  
  Span::add_event(telemetry_span, "message.received", Some([
    ("device.id", StringValue(device_id)),
    ("message.id", StringValue("MSG-001")),
    ("message.type", StringValue("TELEMETRY")),
    ("protocol", StringValue("MQTT"))
  ]))
  
  Span::add_event(telemetry_span, "payload.parsed", Some([
    ("temperature", StringValue("25.5C")),
    ("humidity", StringValue("45.2%")),
    ("battery.level", StringValue("85%")),
    ("signal.strength", StringValue("-45dBm"))
  ]))
  
  Span::add_event(telemetry_span, "data.validated", Some([
    ("validation.status", StringValue("PASSED")),
    ("data.quality", StringValue("GOOD")),
    ("schema.version", StringValue("2.0"))
  ]))
  
  Span::add_event(telemetry_span, "data.stored", Some([
    ("storage.location", StringValue("timeseries.db")),
    ("retention.period", StringValue("90days"))
  ]))
  
  let telemetry_end = Clock::now_unix_nanos(Clock::system())
  let telemetry_latency = (telemetry_end - telemetry_start).to_double() / 1000000.0 // Convert to ms
  
  // Record telemetry metrics
  let telemetry_attrs = Attributes::new()
  Attributes::set(telemetry_attrs, "device.type", StringValue(device_type))
  Attributes::set(telemetry_attrs, "protocol", StringValue("MQTT"))
  Counter::add(telemetry_counter, 1.0, Some(telemetry_attrs))
  
  let latency_attrs = Attributes::new()
  Attributes::set(latency_attrs, "device.type", StringValue(device_type))
  Histogram::record(latency_histogram, telemetry_latency, Some(latency_attrs))
  
  let telemetry_log = LogRecord::new_with_context(
    Debug,
    Some("Telemetry processed from " + device_id + " in " + telemetry_latency.to_string() + "ms"),
    Some(Attributes::new()),
    Some(telemetry_start),
    Some(telemetry_end),
    Some(SpanContext::trace_id(telemetry_context)),
    Some(SpanContext::span_id(telemetry_context)),
    None
  )
  Logger::emit(telemetry_logger, telemetry_log)
  
  Span::end(telemetry_span)
  
  // Step 3: Device monitoring
  let monitoring_span = Tracer::start_span(monitoring_tracer, "device.monitoring")
  let monitoring_context = Span::span_context(monitoring_span)
  
  Span::add_event(monitoring_span, "health.check.performed", Some([
    ("device.id", StringValue(device_id)),
    ("last.seen", StringValue("2025-01-01T12:00:00Z")),
    ("connection.status", StringValue("ONLINE"))
  ]))
  
  Span::add_event(monitoring_span, "metrics.calculated", Some([
    ("uptime.percentage", StringValue("99.9%")),
    ("message.success.rate", StringValue("98.5%")),
    ("average.latency", StringValue("45ms"))
  ]))
  
  Span::add_event(monitoring_span, "anomaly.detected", Some([
    ("anomaly.type", StringValue("TEMPERATURE_SPIKE")),
    ("threshold.exceeded", StringValue("75C")),
    ("duration", StringValue("30s"))
  ]))
  
  Span::add_event(monitoring_span, "alert.generated", Some([
    ("alert.severity", StringValue("WARNING")),
    ("alert.message", StringValue("Temperature spike detected")),
    ("alert.id", StringValue("ALERT-001"))
  ]))
  
  // Record device health
  let health_attrs = Attributes::new()
  Attributes::set(health_attrs, "device.id", StringValue(device_id))
  // Gauge::set(device_health_gauge, 85.0, Some(health_attrs)) // Would use in real implementation
  
  let monitoring_log = LogRecord::new_with_context(
    Warn,
    Some("Device alert: " + device_id + " - Temperature spike detected"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(monitoring_context)),
    Some(SpanContext::span_id(monitoring_context)),
    None
  )
  Logger::emit(monitoring_logger, monitoring_log)
  
  Span::end(monitoring_span)
  
  // Step 4: Telemetry analytics
  let analytics_span = Tracer::start_span(analytics_tracer, "telemetry.analytics")
  let analytics_context = Span::span_context(analytics_span)
  
  Span::add_event(analytics_span, "data.aggregated", Some([
    ("aggregation.window", StringValue("1hour")),
    ("avg.temperature", StringValue("24.8C")),
    ("max.temperature", StringValue("26.2C")),
    ("min.temperature", StringValue("23.1C"))
  ]))
  
  Span::add_event(analytics_span, "trend.analyzed", Some([
    ("trend.direction", StringValue("STABLE")),
    ("trend.significance", StringValue("LOW")),
    ("seasonal.pattern", StringValue("NONE"))
  ]))
  
  Span::add_event(analytics_span, "prediction.generated", Some([
    ("prediction.horizon", StringValue("24hours")),
    ("predicted.temperature", StringValue("25.1C")),
    ("confidence.interval", StringValue("±0.5C"))
  ]))
  
  let analytics_log = LogRecord::new_with_context(
    Info,
    Some("Analytics completed for " + device_id),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(analytics_context)),
    Some(SpanContext::span_id(analytics_context)),
    None
  )
  Logger::emit(analytics_logger, analytics_log)
  
  Span::end(analytics_span)
  
  // Step 5: Device maintenance
  let maintenance_span = Tracer::start_span(maintenance_tracer, "device.maintenance")
  let maintenance_context = Span::span_context(maintenance_span)
  
  Span::add_event(maintenance_span, "maintenance.scheduled", Some([
    ("device.id", StringValue(device_id)),
    ("maintenance.type", StringValue("FIRMWARE_UPDATE")),
    ("scheduled.date", StringValue("2025-01-02T02:00:00Z"))
  ]))
  
  Span::add_event(maintenance_span, "update.started", Some([
    ("firmware.version", StringValue("1.2.4")),
    ("update.size", StringValue("2.5MB")),
    ("update.method", StringValue("OTA"))
  ]))
  
  Span::add_event(maintenance_span, "update.completed", Some([
    ("update.status", StringValue("SUCCESS")),
    ("new.firmware.version", StringValue("1.2.4")),
    ("update.duration", StringValue("5m 30s"))
  ]))
  
  Span::add_event(maintenance_span, "device.restarted", Some([
    ("restart.timestamp", StringValue("2025-01-02T02:05:30Z")),
    ("restart.reason", StringValue("FIRMWARE_UPDATE"))
  ]))
  
  let maintenance_log = LogRecord::new_with_context(
    Info,
    Some("Maintenance completed for " + device_id + " - Updated to firmware 1.2.4"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(maintenance_context)),
    Some(SpanContext::span_id(maintenance_context)),
    None
  )
  Logger::emit(maintenance_logger, maintenance_log)
  
  Span::end(maintenance_span)
  
  // Verify IoT workflow
  assert_eq(device_id, "IOT-DEVICE-001")
  assert_eq(device_type, "TEMPERATURE_SENSOR")
  assert_true(telemetry_latency > 0.0)
  
  // Verify IoT metrics
  assert_eq(device_counter.name, "devices.total")
  assert_eq(telemetry_counter.name, "telemetry.messages")
  assert_eq(latency_histogram.name, "telemetry.latency")
  assert_eq(device_health_gauge.name, "device.health.score")
}